#section FRAGMENT_SHADER
PhongMaterial material;
uniform int material_id;
uniform sampler2D diffuse_tex;
uniform sampler2D specular_tex;
int material_id;
vec3 view_dir;
vec4 diffuse;
float specular;

void standard_init@after(){
  material = materials[material_id];
  view_dir = normalize(camera_position - position);
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

vec4 standard_finish(){
  color.w = diffuse.w;
  if(color.w < material.alpha_cutoff)
    color = vec4(0);
}
