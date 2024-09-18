#section COMPUTE_SHADER
#extension GL_ARB_compute_shader : require
#extension GL_ARB_shader_storage_buffer_object : require
#include (trial::trial "depth.glsl")

uniform sampler2D depth_tex;
uniform float surface_thickness = 1.5;

void simulate_particle(inout Particle particle){
  call_next_method();

  // Check if in view frustum
  vec4 pos_2d = projection_matrix * view_matrix * vec4(particle.position, 1);
  pos_2d.xyz /= pos_2d.w;
  if(-1 < pos_2d.x && pos_2d.x < +1 && -1 < pos_2d.y && pos_2d.y < +1){
    vec2 uv = pos_2d.xy * 0.5 + 0.5;
    ivec2 pix_uv = ivec2(uv * view_size);
    
    float depth = 1.0-texelFetch(depth_tex, pix_uv, 0).x;
    float surface_linear_depth = to_linear_depth(depth, near_plane, far_plane);

    float life_lerp = 1 - (particle.life / particle.max_life);
    float particle_size = mix(particle.size_begin, particle.size_end, life_lerp);
    // Check if within thickness of surface
    if((surface_linear_depth < pos_2d.w + particle_size) && 
       (pos_2d.w - particle_size < surface_linear_depth + surface_thickness)){
      mat4 inv_view_projection_matrix = inv_view_matrix * inv_projection_matrix;
      vec3 surface_normal = depth_normal(depth_tex, uv, inv_view_projection_matrix);
      const float restitution = 0.8;
      particle.velocity = reflect(particle.velocity, surface_normal) * restitution;
    }
  }
}

