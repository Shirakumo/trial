out vec4 color;
in vec2 tex_coord;
// KLUDGE
// float lighting_strength = 1.0;

uniform sampler2D position_map;
uniform sampler2D normal_map;
uniform sampler2D albedo_map;
uniform sampler2D metal_map;
uniform vec3 view_position;
const float PI = 3.14159265;

float NDF_ggx(vec3 N, vec3 H, float a){
  float a2     = a*a;
  float NdotH  = max(dot(N, H), 0.0);
  float NdotH2 = NdotH*NdotH;
  float denom  = (NdotH2 * (a2 - 1.0) + 1.0);
  return a2 / (PI * denom * denom);
}

float G_sggx(float NdotV, float k){
  return NdotV / (NdotV * (1.0 - k) + k);
}
  
float G_smith(vec3 N, vec3 V, vec3 L, float k){
  float ggx1 = G_sggx(max(dot(N, V), 0.0), k);
  float ggx2 = G_sggx(max(dot(N, L), 0.0), k);	
  return ggx1 * ggx2;
}

vec3 F_s(float cos_theta, vec3 F0){
    return F0 + (1.0 - F0) * pow(1.0 - cos_theta, 5.0);
}

float light_attenuation(Light light, vec3 position){
  float distance = length(light.position - position);
  // return 1.0 / (1.0 + light.attenuation_linear * distance + light.attenuation_quadratic * distance * distance);
  return 1.0 / (distance*distance);
}

struct light_data{
  vec3 L;
  vec3 radiance;
};

light_data directional_light(Light light, vec3 position){
  vec3 L = normalize(light.direction);
  vec3 radiance = light.color;
  return light_data(L, radiance);
}

light_data point_light(Light light, vec3 position){
  vec3 L = normalize(light.position - position);
  vec3 radiance = light.color * light_attenuation(light, position);
  return light_data(L, radiance);
}

light_data spot_light(Light light, vec3 position){
  // FIXME
  vec3 L = normalize(light.position - position);
  vec3 radiance = light.color * light_attenuation(light, position);
  return light_data(L, radiance);
}

void main(){
  vec3 position = texture(position_map, tex_coord).rgb;
  vec3 normal = texture(normal_map, tex_coord).rgb;
  vec3 albedo = texture(albedo_map, tex_coord).rgb;
  if(albedo.x <= 0) discard;
  // r = metalness, g = roughness, b = occlusion;
  vec3 metal = texture(metal_map, tex_coord).rgb;
  
  vec3 N = normalize(normal);
  vec3 V = normalize(view_position - position);
  vec3 Lo = vec3(0);
  for(int i=0; i<light_block.count; ++i){
    Light light = light_block.lights[i];
    // Radiance calculation
    light_data data;
    switch(light.type){
    case 0: break;
    case 1: data = directional_light(light, position); break;
    case 2: data = point_light(light, position); break;
    case 3: data = spot_light(light, position); break;
    }
    vec3 L = data.L;
    vec3 H = normalize(V + L);
    vec3 radiance = data.radiance;
    
    // Cook-Torrance BRDF
    vec3 F0 = mix(vec3(0.04), albedo, metal.r);
    vec3 F = F_s(max(dot(H, V), 0.0), F0);
    float NDF = NDF_ggx(N, H, metal.g);
    float G = G_smith(N, V, L, metal.g);
    
    vec3 kS = F;
    vec3 kD = vec3(1.0) - kS;
    kD *= 1.0 - metal.r;
    
    vec3 numerator = NDF * G * F;
    float denominator = 4.0 * max(dot(N, V), 0.0) * max(dot(N, L), 0.0);
    vec3 specular = numerator / max(denominator, 0.001);
    
    // Add to irradiance
    float NdotL = max(dot(N, L), 0.0);
    Lo += (kD * albedo / PI + specular) * radiance * NdotL;
  }
  
  // Add ambient part
  vec3 ambient = vec3(0.03) * albedo * metal.b;
  color = vec4(ambient + (Lo*lighting_strength), 1);
}
