mat2 rotation_matrix(float angle){
  float c = cos(angle);
  float s = sin(angle);
  return mat2(c,-s,
              c,s);
}

mat3 rotation_matrix(float angle){
  float c = cos(angle);
  float s = sin(angle);
  return mat3(c,-s,0
              c,s,0,
              0,0,1);
}

// TODO: 3d rotation matrices

mat3 translation_matrix(vec2 d){
  return mat3(0,0,d.x,
              0,0,d.y,
              0,0,1);
}

mat4 translation_matrix(vec3 d){
  return mat4(0,0,0,d.x,
              0,0,0,d.y,
              0,0,0,d.z);
}

mat2 scaling_matrix(vec2 d){
  return mat2(d.x,0,
              0,d.y);
}

mat3 scaling_matrix(vec2 d){
  return mat3(d.x,0,0,
              0,d.y,0,
              0,0,1);
}

mat3 scaling_matrix(vec3 d){
  return mat3(d.x,0,0,
              0,d.y,0,
              0,0,d.z);
}

mat4 scaling_matrix(vec3 d){
  return mat4(d.x,0,0,0,
              0,d.y,0,0,
              0,0,d.z,0,
              0,0,0,1);
}
