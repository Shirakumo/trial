#include "depth.glsl"

uniform sampler2D depth_map;
uniform float ssr_max_distance = 20.0;
uniform float ssr_distance_falloff = 0.0;
uniform int ssr_step_count = 20;
uniform float ssr_thickness = 0.0;

vec3 evaluate_ssr(in vec2 uv, in vec3 normal){
  vec3 start_pos = depth_world_pos(depth_map, uv, inv_projection_matrix);
  vec3 ray_dir = normalize(reflect(start_pos, normal));
  vec3 end_pos = start_pos + ray_dir * ssr_max_distance;
  vec3 ray_step = ray_dir * (ssr_max_distance / ssr_step_count);
  vec3 ray = start_pos;
  vec2 ray_uv;

  // 1. Find the texel on which we intersect
  for(int i=0; i<ssr_step_count; ++i){
    ray += ray_step;
    ray_uv = to_clip_uv(ray, projection_matrix);
    vec3 depth_pos = depth_world_pos(depth_map, ray_uv, inv_projection_matrix);
    float depth_diff = abs(ray.z - depth_pos.z);
    if(depth_diff < ssr_thickness){ // We found something in front of the ray, stop.
      break;
    }
  }

  // 2. Binary search to find the most precise ray position
  ray_step *= -0.5;
  for(int i=0; i<ssr_step_count && 0.0001 < dot(ray_step,ray_step); ++i){
    ray += ray_step;
    ray_uv = to_clip_uv(ray, projection_matrix);
    vec3 depth_pos = depth_world_pos(depth_map, ray_uv, inv_projection_matrix);
    float depth_diff = abs(ray.z - depth_pos.z);
    if(depth_diff < ssr_thickness){
      ray_step = -abs(ray_step)*0.5;
    } else {
      ray_step = +abs(ray_step)*0.5;
    }
  }

  // 3. Compute visibility factor to blend off reflections
  vec2 border_falloff = 1-clamp(abs(5-ray_uv*10)-4, 0.0, 1.0);

  float visibility
    = (max(0.0, dot(normalize(start_pos), ray_dir)))
    * mix(1.0, distance(end_pos, ray) / ssr_max_distance, ssr_distance_falloff)
    * min(border_falloff.x, border_falloff.y);
  return vec3(ray_uv, visibility);
}
