#section FRAGMENT_SHADER

vec3 view_dir;
vec3 diffuse;
vec3 specular;

void standard_init@after(){
  view_dir = normalize(camera_position - position);
  diffuse = sample_texture(ALBEDO, uv).xyz;
  specular = sample_texture(SPECULAR, uv).xyz;
}

vec4 standard_shade(in StandardLight light){
  StandardLightData light_data = evaluate_light(light);
  vec3 reflect_dir = reflect(-light_data.direction, normal);
  return light_data.radiance
    * (diffuse * max(dot(normal, light_data.direction), 0)
      +specular * pow(max(dot(view_dir, reflect_dir), 0.0), 32));
}
