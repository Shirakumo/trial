#section VERTEX_SHADER
layout (location = 0) in vec3 in_position;
layout (location = 1) in vec3 in_normal;
layout (location = 2) in vec2 in_uv;
uniform mat4 model_matrix;
uniform mat4 inv_model_matrix;
out vec3 v_world_position;
out vec3 v_view_position;
out vec3 v_normal;
out vec2 v_uv;

void main(){
  vec4 world_position = model_matrix * vec4(in_position, 1);
  vec4 view_position = view_matrix * world_position;
  gl_Position = projection_matrix * view_position;
  v_world_position = world_position.xyz;
  v_view_position = view_position.xyz;
  v_normal = normalize(mat3(transpose(inv_model_matrix)) * in_normal);
  v_uv = in_uv;
}

#section FRAGMENT_SHADER
uniform mat4 model_matrix;
uniform mat4 inv_model_matrix;
in vec3 v_world_position;
in vec3 v_view_position;
in vec3 v_normal;
in vec2 v_uv;

void standard_init@before(){
  world_position = v_world_position;
  view_position = v_view_position;
  normal = v_normal;
  uv = v_uv;
}
