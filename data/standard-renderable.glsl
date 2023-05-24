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

void standard_init@after(){
  uv = v_uv;
  normal = v_normal;
  position = v_position;
}
