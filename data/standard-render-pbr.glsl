#section FRAGMENT_SHADER
#include (trial::trial "normal_map.glsl")

PbrMaterial material;
uniform int material_id;
uniform sampler2D albedo_tex;
uniform sampler2D metal_rough_occlusion_tex;
uniform sampler2D normal_tex;
vec3 view_dir, F0;
vec4 albedo;
float metallic, roughness, occlusion;

const float PI = 3.14159265359;

float NDF_ggx(vec3 N, vec3 H, float roughness){
  float a      = roughness*roughness;
  float a2     = a*a;
  float NdotH  = max(dot(N, H), 0.0);
  float NdotH2 = NdotH*NdotH;
  float denom  = (NdotH2 * (a2 - 1.0) + 1.0);
  return a2 / (PI * denom * denom);
}

float G_sggx(float NdotV, float roughness){
  float r = (roughness + 1.0);
  float k = (r*r) / 8.0;
  return NdotV / (NdotV * (1.0 - k) + k);
}
  
float G_smith(vec3 N, vec3 V, vec3 L, float roughness){
  float ggx1 = G_sggx(max(dot(N, V), 0.0), roughness);
  float ggx2 = G_sggx(max(dot(N, L), 0.0), roughness);	
  return ggx1 * ggx2;
}

vec3 F_s(float cos_theta, vec3 F0){
  return F0 + (1.0 - F0) * pow(1.0 - cos_theta, 5.0);
}

void standard_init(){
  maybe_call_next_method();
  material = materials[material_id];
  normal = normal_map(normal_tex, world_position-camera_position, uv, normal);
  view_dir = normalize(camera_position - world_position);
  albedo = texture(albedo_tex, uv) * material.albedo_factor;
  vec3 mro = texture(metal_rough_occlusion_tex, uv).xyz;
  metallic = mro.x * material.metallic_factor;
  roughness = mro.y * material.roughness_factor;
  occlusion = 1.0 - (mro.z * material.occlusion_factor);
  F0 = mix(vec3(0.04), albedo.xyz, metallic);
}

vec4 standard_shade(in StandardLight light){
  if(light.type == 1){
    return vec4(occlusion * albedo.xyz * light.color, 1);
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
    kD *= 1.0 - metallic;
    
    vec3 numerator = NDF * G * F;
    float denominator = 4.0 * max(dot(N, V), 0.0) * max(dot(N, L), 0.0);
    vec3 specular = numerator / max(denominator, 0.001);
  
    float NdotL = max(dot(N, L), 0.0);
    return vec4((kD * albedo.xyz / PI + specular) * radiance * NdotL, 1);
  }
}

void standard_finish(){
  color.w = albedo.w;
  if(color.w < material.alpha_cutoff)
    discard;
}
