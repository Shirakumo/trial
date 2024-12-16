vec3 to_world_position(in vec2 uv, in float z, in mat4 inv_view_projection_matrix){
  float x = uv.x * 2.0f - 1.0f;
  float y = uv.y * 2.0f - 1.0f;
  vec4 position_s = vec4(x, y, z, 1.0f);
  vec4 position_v = inv_view_projection_matrix * position_s;
  return position_v.xyz / position_v.w;
}

vec2 to_clip_uv(in vec3 pos, in mat4 projection_matrix){
  vec4 offset = vec4(pos, 1.0);
  offset = projection_matrix * offset;
  offset.xy /= offset.w;
  return offset.xy * 0.5 + 0.5;
}

vec2 to_clip_uv_clamped(in vec3 pos, in mat4 projection_matrix){
  vec4 offset = vec4(pos, 1.0);
  offset = projection_matrix * offset;
  offset.xy /= offset.w;
  // Clamp by projecting from center.
  if(offset.z < 0.0){
    offset.xy /= -max(abs(offset.x), abs(offset.y));
  }
  offset.xy = offset.xy * 0.5 + 0.5;
  return offset.xy;
}
