vec3 to_world_position(in vec2 uv, in float z, in mat4 inv_view_projection_matrix){
  float x = uv.x * 2.0f - 1.0f;
  float y = (1.0 - uv.y) * 2.0f - 1.0f;
  vec4 position_s = vec4(x, y, z, 1.0f);
  vec4 position_v = inv_view_projection_matrix * position_s;
  return position_v.xyz / position_v.w;
}
