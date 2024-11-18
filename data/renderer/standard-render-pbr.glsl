#section FRAGMENT_SHADER
#include (trial::trial "normal-map.glsl")
#include "pbr-common.glsl"

PbrMaterial material;
uniform int material_id;
uniform sampler2D albedo_tex;
uniform sampler2D metal_rough_occlusion_tex;
uniform sampler2D normal_tex;
uniform sampler2D emission_tex;
uniform sampler2D brdf_lut;
uniform samplerCube irradiance_map;
uniform samplerCube environment_map;
vec3 view_dir, F0, emission;
vec4 albedo;
float metalness, roughness, occlusion;

const float PI = 3.14159265359;
const float MAX_REFLECTION_LOD = 4.0;

void standard_init(){
  maybe_call_next_method();
  material = materials[material_id];
  normal = normal_map(normal_tex, world_position-camera_position, uv, normal);
  emission = texture(emission_tex, uv).xyz * material.emission_factor;
  view_dir = normalize(camera_position - world_position);
  albedo = texture(albedo_tex, uv);
  albedo.xyz = pow(albedo.xyz,vec3(gamma));
  albedo *= material.albedo_factor;
  vec3 mro = texture(metal_rough_occlusion_tex, uv).xyz;
  metalness = mro.x * material.metalness_factor;
  roughness = mro.y * material.roughness_factor;
  occlusion = 1.0 - (mro.z * material.occlusion_factor);
}

void standard_init@after(){
  F0 = mix(vec3(0.04), albedo.xyz, metalness);
}

vec4 standard_shade(in StandardLight light){
  if(light.type == 1){
    return vec4(occlusion * albedo.xyz * light.color, 1);
  }else if(light.type == 250){
    vec3 N = normal;
    vec3 V = view_dir;
    vec3 R = reflect(-V, N);

    vec3 F = F_s_r(max(dot(N, V), 0.0), F0, roughness);
    vec3 kS = F;
    vec3 kD = 1.0 - kS;
    kD *= 1.0 - metalness;

    vec3 irradiance = texture(irradiance_map, vec3(N.x, -N.y, N.z)).rgb;
    vec3 diffuse = irradiance * albedo.xyz;

    vec2 BRDFuv = vec2(max(dot(N, V), 0.0), roughness);
    vec2 envBRDF = texture(brdf_lut, vec2(BRDFuv.x, -BRDFuv.y)).rg;
    vec3 prefiltered_color = textureLod(environment_map, vec3(R.x, -R.y, R.z),  roughness * MAX_REFLECTION_LOD).rgb;
    vec3 specular = prefiltered_color * (F * envBRDF.x + envBRDF.y);

    return vec4((kD * diffuse + specular) * occlusion * light.color, 1);
  }else{
    StandardLightData light_data = evaluate_light(light);
    vec3 N = normal;
    vec3 V = view_dir;
    vec3 L = light_data.direction;
    vec3 H = normalize(V + L);
    vec3 radiance = light_data.radiance;

    // Cook-Torrance BRDF
    float NDF = NDF_ggx(N, H, roughness);
    float G = G_smith(N, V, L, roughness);
    vec3 F = F_s(max(dot(H, V), 0.0), F0);

    vec3 kS = F;
    vec3 kD = vec3(1.0) - kS;
    kD *= 1.0 - metalness;

    vec3 numerator = NDF * G * F;
    float denominator = 4.0 * max(dot(N, V), 0.0) * max(dot(N, L), 0.0);
    vec3 specular = numerator / max(denominator, 0.001);

    float NdotL = max(dot(N, L), 0.0);
    return vec4((kD * albedo.xyz / PI + specular) * radiance * NdotL, 1);
  }
}

void standard_finish(){
  color.rgb += emission * 5.0;
  color.w = albedo.w;
  if(color.w < material.alpha_cutoff)
    discard;
}
