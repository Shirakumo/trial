float to_linear_depth(float z_b, float near_plane, float far_plane){
  float z_n = 2.0 * z_b - 1.0;
  return 2.0 * far_plane * near_plane / (near_plane + far_plane - z_n * (near_plane - far_plane));
}
