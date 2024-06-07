#include (trial::trial "depth.glsl")

uniform float ssr_max_distance = 15.0;
uniform float ssr_distance_falloff = 0.2;
uniform int ssr_step_count = 40;
uniform float ssr_thickness = 2.0;

vec2 random2(vec2 p) {
  vec3 p3 = fract(p.xyx * vec3(443.897, 441.423, .0973));
  p3 += dot(p3, p3.yzx + 19.19);
  return fract((p3.xx + p3.yz) * p3.zy);
}

vec3 evaluate_ssr(in sampler2D depth_map, in vec2 uv, in vec3 normal){
  vec3 start_pos = depth_world_pos(depth_map, uv, inv_projection_matrix);
  vec3 ray_dir = normalize(reflect(start_pos, normal));

  vec3 end_pos = start_pos + ray_dir * ssr_max_distance;
  vec3 ray_step = (end_pos - start_pos) / ssr_step_count;
  vec3 ray = start_pos + ray_step * 0.1;
  vec2 ray_uv;

  // Raymarch to find initial hit
  float current_depth = ray.z;
  for(int i=0; i<ssr_step_count; ++i){
    ray += ray_step;
    ray_uv = to_clip_uv(ray, projection_matrix);
    vec3 depth_pos = depth_world_pos(depth_map, ray_uv, inv_projection_matrix);
    float depth_diff = depth_pos.z - ray.z;
    if(0.0 < depth_diff && depth_diff < (ray.z - current_depth) + ssr_thickness){
      break;
    }
    current_depth = ray.z;
  }

  // Compute visibility factor to blend off reflections
  vec2 border_falloff = 1-clamp(abs(5-ray_uv*10)-4, 0.0, 1.0);
  ray_uv += random2(uv)/ssr_step_count;

  float visibility
    = (max(0.0, dot(normalize(start_pos), ray_dir)))
    * mix(1.0, distance(end_pos, ray) / ssr_max_distance, ssr_distance_falloff)
    * min(border_falloff.x, border_falloff.y);
  return vec3(ray_uv, visibility);
}

vec3 evaluate_ssr_(in sampler2D depth_map, in vec2 uv){
  return evaluate_ssr(depth_map, uv, depth_normal(previous_depth, uv, inv_projection_matrix));
}
