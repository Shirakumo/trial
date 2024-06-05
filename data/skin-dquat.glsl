uniform sampler1DArray pose;

mat2x4 pose_matrix(in int i){
  return transpose(mat4x2(
    texelFetch(pose, ivec2(0, i), 0),
    texelFetch(pose, ivec2(1, i), 0)));
}

vec4 quat_mul(vec4 q1, vec4 q2){
  return vec4(q2.x*q1.w + q2.y*q1.z - q2.z*q1.y + q2.w*q1.x,
             -q2.x*q1.z + q2.y*q1.w + q2.z*q1.x + q2.w*q1.y,
              q2.x*q1.y - q2.y*q1.x + q2.z*q1.w + q2.w*q1.z,
             -q2.x*q1.x - q2.y*q1.y - q2.z*q1.z + q2.w*q1.w);
}

vec4 dquat_vector(mat2x4 dq, vec3 v){
  vec4 real = dq[0];
  vec3 r_vector = real.xyz;
  float r_scalar = real.w;
  vec3 rotated = r_vector*2*dot(r_vector, v)
    + v*(r_scalar*r_scalar - dot(r_vector, r_vector))
    + cross(r_vector, v)*2*r_scalar;
  return vec4(rotated, 0);
}

vec4 dquat_point(mat2x4 dq, vec3 v){
  vec4 real = dq[0];
  vec4 dual = dq[1];
  vec3 rotated = dquat_vector(dq, v).xyz;
  vec4 conjugate = vec4(-real.xyz, real.w);
  vec3 t = quat_mul(conjugate, dual*2).xyz;
  return vec4(rotated+t, 1);
}

mat2x4 dquat_normalized(mat2x4 dq){
  float inv_mag = 1.0 / length(dq[0]);
  dq[0] *= inv_mag;
  dq[1] *= inv_mag;
  return dq;
}

void skin_vertex(inout vec3 position, inout vec3 normal, in vec4 joints, in vec4 weights){
  ivec4 j = ivec4(joints);
  mat2x4 skin_dq = dquat_normalized(
                 + (pose_matrix(j.x) * weights.x)
                 + (pose_matrix(j.y) * weights.y)
                 + (pose_matrix(j.z) * weights.z)
                 + (pose_matrix(j.w) * weights.w));
  position = vec3(dquat_point(skin_dq, position));
  normal = vec3(dquat_vector(skin_dq, normal));
}
