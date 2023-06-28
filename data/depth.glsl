#include "positioning.glsl"

float to_linear_depth(in float z_b, in float near_plane, in float far_plane){
  float z_n = 2.0 * z_b - 1.0;
  return 2.0 * far_plane * near_plane / (near_plane + far_plane - z_n * (near_plane - far_plane));
}

vec3 depth_normal(sampler2D depth_tex, in vec2 uv, in mat4 inv_view_projection_matrix){
  vec2 inv_size = vec2(1.0)/vec2(textureSize(depth_tex, 0).xy);

  vec2 uv0 = uv;
  vec2 uv1 = uv + vec2(1, 0) * inv_size;
  vec2 uv2 = uv + vec2(0, 1) * inv_size;
  
  float depth0 = texture(depth_tex, uv0).x;
  float depth1 = texture(depth_tex, uv1).x;
  float depth2 = texture(depth_tex, uv2).x;
  
  vec3 P0 = to_world_position(uv0, depth0, inv_view_projection_matrix);
  vec3 P1 = to_world_position(uv1, depth1, inv_view_projection_matrix);
  vec3 P2 = to_world_position(uv2, depth2, inv_view_projection_matrix);
  
  return normalize(cross(P1 - P0, P2 - P0));
}
