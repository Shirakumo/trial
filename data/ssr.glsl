#include "depth.glsl"

uniform sampler2D depth_map;
uniform float ssr_max_distance = 15.0;
uniform float ssr_ray_resolution = 0.3;
uniform float ssr_distance_falloff = 0.0;
uniform int ssr_step_count = 20;
uniform float ssr_thickness = 0.2;

vec3 evaluate_ssr(in vec2 uv, in vec3 normal){
  vec3 start_pos = depth_world_pos(depth_map, uv, inv_projection_matrix);
  vec3 ray_dir = normalize(reflect(start_pos, normal));
  vec3 end_pos = start_pos + ray_dir * ssr_max_distance;
  vec3 ray_step = ray_dir * (ssr_max_distance / ssr_step_count);
  vec3 ray = start_pos;

  float depth_step = abs(ray_step.z);
  for(int i=0; i<ssr_step_count; ++i){
    ray += ray_step;
    vec2 ray_uv = to_clip_uv(ray, projection_matrix);
    vec3 depth_pos = depth_world_pos(depth_map, ray_uv, inv_projection_matrix);

    float depth_diff = (ray.z - depth_pos.z);
    if(0 < depth_diff && depth_diff < ssr_thickness+depth_step){
      vec2 border_falloff = 1-clamp(abs(5-ray_uv*10)-4, 0.0, 1.0);

      float visibility
        = (1 - max(0.0, dot(-normalize(start_pos), ray_dir)))
        * (1 - distance(end_pos, ray) / ssr_max_distance)
        * (1 - clamp(depth_diff / ssr_thickness, 0.0, 1.0))
        * min(border_falloff.x, border_falloff.y);

      return vec3(ray_uv, visibility);
    }
  }
  return vec3(0);
}
