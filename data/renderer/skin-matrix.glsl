uniform sampler1DArray pose;

mat4 pose_matrix(in int i){
  return transpose(mat4(
    texelFetch(pose, ivec2(0, i), 0),
    texelFetch(pose, ivec2(1, i), 0),
    texelFetch(pose, ivec2(2, i), 0),
    vec4(0,0,0,1)));
}

void skin_vertex(inout vec3 position, inout vec3 normal, in vec4 joints, in vec4 weights){
  ivec4 j = ivec4(joints);
  mat4 skin_matrix = (pose_matrix(j.x) * weights.x)
                   + (pose_matrix(j.y) * weights.y)
                   + (pose_matrix(j.z) * weights.z)
                   + (pose_matrix(j.w) * weights.w);
  position = vec3(skin_matrix * vec4(position, 1.0f));
  normal = vec3(skin_matrix * vec4(normal, 0.0f));
}
