uniform sampler2D previous_pass;
uniform sampler2D depth_map;
uniform float ssr_max_distance = 100.0;
uniform float ssr_distance_falloff = 0.0;
const int ssr_step_count = 16;

vec4 evaluate_ssr(in vec2 uv, in vec3 normal){
  vec3 frag_pos = depth_world_pos(depth_map, uv, inv_projection_matrix);
  vec3 ray_dir = normalize(reflect(frag_pos, normal));
  vec3 ray_step = ray_dir * ssr_max_distance / ssr_step_count;
  vec3 sample_pos = frag_pos;
  for(int i=0; i<step_count; ++i){
    sample_pos += ray_step;
    
    vec4 offset = vec4(sample_pos, 1.0);
    offset = projection_matrix * offset;
    offset.xy /= offset.w;
    offset.xy = offset.xy * 0.5 + 0.5;

    float sample_depth = depth_world_pos(depth_map, offset.xy, inv_projection_matrix).z;
    if(sample_pos.z < sample_depth){
      return vec4(texture(previous_pass, offset.xy).rgb, (i*ssr_distance_falloff)/step_count);
    }
  }
  return vec4(0);
}
