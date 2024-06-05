#section VERTEX_SHADER
layout (location = 0) in vec3 in_position;
layout (location = 1) in vec3 in_normal;
layout (location = 2) in vec2 in_uv;
layout (location = 5) in vec4 in_joints;
layout (location = 6) in vec4 in_weights;
uniform mat4 model_matrix;
uniform mat4 inv_model_matrix;
out vec3 v_world_position;
out vec3 v_view_position;
out vec3 v_normal;
out vec2 v_uv;

void main(){
  vec3 position = in_position;
  vec3 normal = in_normal;
  vec4 uv = in_uv;

  morph_vertex(position, normal, uv);
  skin_vertex(position, normal, in_joints, in_weights);

  vec4 world_position = model_matrix * vec4(position, 1.0f);
  vec4 view_position = view_matrix * world_position;
  gl_Position = projection_matrix * view_position;

  v_world_position = world_position.xyz;
  v_view_position = view_position.xyz;
  v_normal = vec3(model_matrix * vec4(normal, 0.0f));
  v_uv = uv;
}
