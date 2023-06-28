#section COMPUTE_SHADER
#include (trial::trial "depth.glsl")
#include (trial::trial "positioning.glsl")

uniform sampler2D depth_tex;

void simulate_particle(inout Particle particle){
  call_next_method();

  // Check if in view frustum
  vec4 pos_2d = projection_matrix * view_matrix * vec4(particle.position, 1);
  pos_2d.xyz /= pos_2d.w;
  if(-1 < pos_2d.x && pos_2d.x < +1 && -1 < pos_2d.y && pos_2d.y < +1){
    vec2 uv = pos_2d.xy * vec2(0.5, -0.5) + 0.5;
    ivec2 pix_uv = ivec2(uv * view_size);
    
    float depth = 1.0-texelFetch(depth_tex, pix_uv, 0).x;
    float surface_linear_depth = to_linear_depth(depth, near_plane, far_plane);
    float surface_thickness = 1.5;

    float life_lerp = 1 - (particle.life / particle.max_life);
    float particle_size = mix(particle.size_begin, particle.size_end, life_lerp);
    // Check if within thickness of surface
    if((surface_linear_depth < pos_2d.w + particle_size) && 
       (pos_2d.w - particle_size < surface_linear_depth + surface_thickness)){
      float depth_x = 1.0-texelFetch(depth_tex, pix_uv + ivec2(1, 0), 0).x;
      float depth_y = 1.0-texelFetch(depth_tex, pix_uv + ivec2(0, 1), 0).x;
      mat4 inv_view_projection_matrix = inv_view_matrix * inv_projection_matrix;
      vec3 p0 = to_world_position(uv, depth, inv_view_projection_matrix);
      vec3 p1 = to_world_position(uv, depth_x, inv_view_projection_matrix);
      vec3 p2 = to_world_position(uv, depth_y, inv_view_projection_matrix);
      vec3 surface_normal = normalize(cross(p2-p0, p1-p0));

      const float restitution = 0.9;
      particle.velocity = reflect(particle.velocity, surface_normal) * restitution;
    }
  }
}
