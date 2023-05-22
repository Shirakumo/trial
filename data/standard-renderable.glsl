#section VERTEX_SHADER
layout (location = 0) in vec3 in_position;
layout (location = 1) in vec2 in_uv;
layout (location = 2) in vec3 in_normal;

void main(){
  vec4 position = projection_matrix * view_matrix * vec4(in_position, 1);
  gl_Position = position;
  v_position = position.xyz;
  v_uv = in_uv;
  v_normal = in_normal;
}

#section FRAGMENT_SHADER
uniform int material_id;
uniform sampler2D[32] textures;

vec2 uv;
vec3 normal;
vec4 color;
StandardMaterial material;

void standard_init();
vec4 standard_shade(in StandardLight light);
vec4 standard_mix(in vec4 upper, in vec4 lower);
void standard_finish();

#define NONE -1
#define ALBEDO 0
#define NORMAL 1
#define EMISSION 2
#define METAL_ROUGH_OCCLUSION 3
#define METALLIC 4
#define ROUGHNESS 5
#define OCCLUSION 6

vec4 sample_texture(int id, vec2 uv){
  switch(id){
  case NONE: return vec4(0);
  case ALBEDO:
  case NORMAL:
  case METAL_ROUGH_OCCLUUSION:
  case EMISSION: return texture(textures[material.textures[id]], uv);
  case METALLIC: return texture(textures[material.textures[METAL_ROUGH_OCCLUSION]], uv).xxxx;
  case ROUGHNESS: return texture(textures[material.textures[METAL_ROUGH_OCCLUSION]], uv).yyyy;
  case OCCLUSION: return texture(textures[material.textures[METAL_ROUGH_OCCLUSION]], uv).zzzz;
  default: return vec4(1,0,0,1);
  }
}

void standard_init(){
  uv = v_uv;
  normal = v_normal;
  color = vec4(0);
  material = materials[material_id];
}

vec4 standard_shade(in StandardLight light){
  return sample_texture(ALBEDO, uv);
}

vec4 standard_mix(in vec4 upper, in vec4 lower){
  return upper + lower;
}

void standard_finish(){}

void main(){
  standard_init(material, color);
  for(int light_idx = 0; light_idx<light_Count; ++light_idx){
    StandardLight light = lights[light_idx];
    vec4 local_color;
    color = standard_mix(material, standard_shade(material, light, local_color), color);
  }
  standard_finish(material);
  f_color = color;
  f_normal = normal;
}
