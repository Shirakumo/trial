vec3 to_world_position(in vec2 uv, in float z, in mat4 inv_view_projection_matrix){
  float x = uv.x * 2.0f - 1.0f;
  float y = uv.y * 2.0f - 1.0f;
  vec4 position_s = vec4(x, y, z, 1.0f);
  vec4 position_v = inv_view_projection_matrix * position_s;
  return position_v.xyz / position_v.w;
}

vec3 to_clip(in vec3 pos, in mat4 projection_matrix){
  vec4 offset = vec4(pos, 1.0);
  offset = projection_matrix * offset;
  return offset.xyz / offset.w;
}

vec4 to_clip_clamped(in vec3 pos, in mat4 projection_matrix){
  vec4 offset = vec4(pos, 1.0);
  offset = projection_matrix * offset;
  offset.xyz /= offset.w;
  // Clamp by projecting from center.
  if(offset.w <= 0.0)
    offset.xy /= -max(abs(offset.x), abs(offset.y));
  return offset;
}

vec2 to_clip_uv(in vec3 pos, in mat4 projection_matrix){
  return to_clip(pos, projection_matrix).xy * 0.5 + 0.5;
}

vec2 to_clip_uv_clamped(in vec3 pos, in mat4 projection_matrix){
  return to_clip_clamped(pos, projection_matrix).xy * 0.5 + 0.5;
}
