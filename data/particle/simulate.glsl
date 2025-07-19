#section COMPUTE_SHADER
#extension GL_ARB_compute_shader : require
#extension GL_ARB_shader_storage_buffer_object : require

layout (local_size_x = SIMULATE_THREADS, local_size_y = 1, local_size_z = 1) in;
#define FORCEFIELDS 32
uint field_count = 0u;
shared ParticleForceField s_force_fields[FORCEFIELDS];

#include (eval (trial::particle-force-field-shader))

void simulate_particle(inout Particle particle){
  for(uint i=0u; i<field_count; ++i){
    ParticleForceField field = s_force_fields[i];
    vec3 force = evaluate_force_field(field, particle);
    particle.velocity += force * dt;
  }
}

void simulate_particle@after(inout Particle particle){
  particle.position += particle.velocity * dt;
  particle.life -= dt;
}

void particle_tick(uint id, inout Particle particle){
  simulate_particle(particle);
  particles[id] = particle;
  
  uint new_index = atomicAdd(draw_args.x, 6u) / 6u;
  alive_particles_1[new_index] = id;
}

void main(){
  uint alive = alive_count;
  field_count = uint(min(particle_force_field_count, FORCEFIELDS));
  if(gl_LocalInvocationIndex < field_count){
    uint id = gl_LocalInvocationIndex;
    s_force_fields[id] = particle_force_fields[id];
  }
  groupMemoryBarrier();
  barrier();

  if(gl_GlobalInvocationID.x < alive){
    uint id = alive_particles_0[gl_GlobalInvocationID.x];
    Particle particle = particles[id];
    if(0 < particle.life){
      particle_tick(id, particle);
    }else{
      uint dead = atomicAdd(dead_count, +1u);
      dead_particles[dead] = id;
    }
  }
}
