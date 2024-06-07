#section FRAGMENT_SHADER
#include (trial::trial "normal-map.glsl")

PhongMaterial material;
uniform int material_id;
uniform sampler2D diffuse_tex;
uniform sampler2D specular_tex;
uniform sampler2D normal_tex;
vec3 view_dir;
vec4 diffuse;
float specular;

void standard_init@after(){
  material = materials[material_id];
  normal = normal_map(normal_tex, world_position-camera_position, uv, normal);
  view_dir = normalize(camera_position - world_position);
  diffuse = texture(diffuse_tex, uv) * material.diffuse_factor;
  specular = texture(specular_tex, uv).x * material.specular_factor;
}

vec4 standard_shade(in StandardLight light){
  StandardLightData light_data = evaluate_light(light);
  vec3 reflect_dir = reflect(-light_data.direction, normal);
  return vec4(light_data.radiance
              * (diffuse.xyz * max(dot(normal, light_data.direction), 0)
                 + specular * pow(max(dot(view_dir, reflect_dir), 0.0), 32)),
              1.0);
}

void standard_finish(){
  color.w = diffuse.w;
  if(color.w < material.alpha_cutoff)
    discard;
}
