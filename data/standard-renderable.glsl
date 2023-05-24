#section VERTEX_SHADER
layout (location = 0) in vec3 in_position;
layout (location = 1) in vec2 in_uv;
layout (location = 2) in vec3 in_normal;
out vec3 v_position;
out vec3 v_normal;
out vec2 v_uv;

void main(){
  vec4 position = projection_matrix * view_matrix * vec4(in_position, 1);
  gl_Position = position;
  v_position = position.xyz;
  v_uv = in_uv;
  v_normal = in_normal;
}

#section FRAGMENT_SHADER
in vec3 v_position;
in vec3 v_normal;
in vec3 v_uv;
uniform int material_id;
StandardMaterial material;

void standard_init(){
  uv = v_uv;
  normal = v_normal;
  position = v_position;
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
