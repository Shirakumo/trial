vec4 quat_mul(vec4 q1, vec4 q2){
  return vec4(q2.x*q1.w + q2.y*q1.z - q2.z*q1.y + q2.w*q1.x,
             -q2.x*q1.z + q2.y*q1.w + q2.z*q1.x + q2.w*q1.y,
              q2.x*q1.y - q2.y*q1.x + q2.z*q1.w + q2.w*q1.z,
             -q2.x*q1.x - q2.y*q1.y - q2.z*q1.z + q2.w*q1.w);
}

vec4 quat2_vector(mat2x4 dq, vec3 v){
  vec4 real = dq[0];
  vec3 r_vector = real.xyz;
  float r_scalar = real.w;
  vec3 rotated = r_vector*2*dot(r_vector, v)
    + v*(r_scalar*r_scalar - dot(r_vector, r_vector))
    + cross(r_vector, v)*2*r_scalar;
  return vec4(rotated, 0);
}

vec4 quat2_point(mat2x4 dq, vec3 v){
  vec4 real = dq[0];
  vec4 dual = dq[1];
  vec3 rotated = quat2_vector(dq, v).xyz;
  vec4 conjugate = vec4(-real.xyz, real.w);
  vec3 t = quat_mul(conjugate, dual*2).xyz;
  return vec4(rotated+t, 1);
}

mat2x4 quat2_normalized(mat2x4 dq){
  float inv_mag = 1.0 / length(dq[0]);
  dq[0] *= inv_mag;
  dq[1] *= inv_mag;
  return dq;
}
