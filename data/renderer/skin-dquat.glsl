#section VERTEX_SHADER
uniform sampler1DArray pose;
#include (trial::trial "quaternions.glsl")

mat2x4 pose_matrix(in int i){
  return transpose(mat4x2(
    texelFetch(pose, ivec2(0, i), 0),
    texelFetch(pose, ivec2(1, i), 0)));
}

void skin_vertex(inout vec3 position, inout vec3 normal, in vec4 joints, in vec4 weights){
  ivec4 j = ivec4(joints);
  mat2x4 skin_dq = quat2_normalized(
                 + (pose_matrix(j.x) * weights.x)
                 + (pose_matrix(j.y) * weights.y)
                 + (pose_matrix(j.z) * weights.z)
                 + (pose_matrix(j.w) * weights.w));
  position = vec3(quat2_point(skin_dq, position));
  normal = vec3(quat2_vector(skin_dq, normal));
}
