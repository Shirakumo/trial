#section VERTEX_SHADER
layout (location = 0) in vec3 in_position;
layout (location = 3) in vec4 in_joints;
layout (location = 4) in vec4 in_weights;
uniform mat4 model_matrix;
uniform int shadow_map_id;
uniform int pose_map;

uniform sampler1DArray pose;
mat4 pose_matrix(in int i){
  return transpose(mat4(
    texelFetch(pose, ivec2(0, i), 0),
    texelFetch(pose, ivec2(1, i), 0),
    texelFetch(pose, ivec2(2, i), 0),
    vec4(0,0,0,1)));
}

void main(){
  mat4 skin_matrix = mat4(1);
  if(0 < pose_map){
    ivec4 j = ivec4(in_joints);
    skin_matrix = (pose_matrix(j.x) * in_weights.x)
                + (pose_matrix(j.y) * in_weights.y)
                + (pose_matrix(j.z) * in_weights.z)
                + (pose_matrix(j.w) * in_weights.w);
  }
  vec4 world_position = model_matrix * skin_matrix * vec4(in_position, 1);
  gl_Position = shadow_info[shadow_map_id].projection_matrix * world_position;
}

#section FRAGMENT_SHADER
void main(){}
