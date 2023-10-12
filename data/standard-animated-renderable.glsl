#section VERTEX_SHADER
layout (location = 0) in vec3 in_position;
layout (location = 1) in vec3 in_normal;
layout (location = 2) in vec2 in_uv;
layout (location = 3) in vec4 in_joints;
layout (location = 4) in vec4 in_weights;
uniform mat4 model_matrix;
uniform mat4 inv_model_matrix;
out vec3 v_world_position;
out vec3 v_view_position;
out vec3 v_normal;
out vec2 v_uv;

uniform mat4 pose[100];

void main(){
  ivec4 j = ivec4(in_joints);
  mat4 skin_matrix = (pose[j.x] * in_weights.x)
                   + (pose[j.y] * in_weights.y)
                   + (pose[j.z] * in_weights.z)
                   + (pose[j.w] * in_weights.w);
  vec4 world_position = model_matrix * skin_matrix * vec4(in_position, 1.0f);
  vec4 view_position = view_matrix * world_position;
  gl_Position = projection_matrix * view_position;
  v_world_position = world_position.xyz;
  v_view_position = view_position.xyz;
  v_normal = vec3(model_matrix * skin_matrix * vec4(in_normal, 0.0f));
  //v_normal = normalize(mat3(transpose(inv_model_matrix)) * in_normal);
  v_uv = in_uv;
}
