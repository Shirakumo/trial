float to_linear_depth(in float z_b, in float near_plane, in float far_plane){
  float z_n = 2.0 * z_b - 1.0;
  return 2.0 * far_plane * near_plane / (near_plane + far_plane - z_n * (near_plane - far_plane));
}

vec3 depth_normal(sampler2D depth_tex, in ivec2 tex, in mat4 inv_view_projection_matrix){
  vec2 size = vec2(1.0)/vec2(textureSize(depth_tex, 0).xy);
  
  float c0 = texelFetch(depth_tex, tex           , 0).w;
  float l2 = texelFetch(depth_tex, tex-ivec2(2,0), 0).w;
  float l1 = texelFetch(depth_tex, tex-ivec2(1,0), 0).w;
  float r1 = texelFetch(depth_tex, tex+ivec2(1,0), 0).w;
  float r2 = texelFetch(depth_tex, tex+ivec2(2,0), 0).w;
  float b2 = texelFetch(depth_tex, tex-ivec2(0,2), 0).w;
  float b1 = texelFetch(depth_tex, tex-ivec2(0,1), 0).w;
  float t1 = texelFetch(depth_tex, tex+ivec2(0,1), 0).w;
  float t2 = texelFetch(depth_tex, tex+ivec2(0,2), 0).w;
    
  float dl = abs(l1*l2/(2.0*l2-l1)-c0);
  float dr = abs(r1*r2/(2.0*r2-r1)-c0);
  float db = abs(b1*b2/(2.0*b2-b1)-c0);
  float dt = abs(t1*t2/(2.0*t2-t1)-c0);
  
  vec3 ce = to_world_position(tex*size, c0, inv_view_projection_matrix);
  
  vec3 dpdx = (dl<dr)
    ? ce-to_world_position((tex-ivec2(1,0))*size, l1, inv_view_projection_matrix)
    : -ce+to_world_position((tex+ivec2(1,0))*size, r1, inv_view_projection_matrix);
  vec3 dpdy = (db<dt)
    ? ce-to_world_position((tex-ivec2(0,1))*size, b1, inv_view_projection_matrix)
    : -ce+to_world_position((tex+ivec2(0,1))*size, t1, inv_view_projection_matrix);

  return normalize(cross(dpdx,dpdy));
}
