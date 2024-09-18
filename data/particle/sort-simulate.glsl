#section COMPUTE_SHADER
#extension GL_ARB_compute_shader : require
#extension GL_ARB_shader_storage_buffer_object : require

void particle_tick(uint id, inout Particle particle){
  simulate_particle(particle);
  particles[id] = particle;
  
  uint new_index = atomicAdd(draw_args.x, 6) / 6;
  alive_particles_1[new_index] = id;
  
  vec3 eye = particle.position - camera_position;
  float dist_2 = dot(eye, eye);
  particle_distances[new_index] = dist_2;
}
