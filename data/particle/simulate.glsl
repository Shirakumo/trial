#section COMPUTE_SHADER
layout (local_size_x = SIMULATE_THREADS, local_size_y = 1, local_size_z = 1) in;
#define FORCEFIELDS 32
shared ParticleForceField s_force_fields[FORCEFIELDS];

vec3 evaluate_force_field(ParticleForceField field, Particle particle){
  vec3 dir = field.position - particle.position;
  float dist = 0.0;
  switch(field.type){
  case 0:
    dist = length(dir);
    break;
  case 1:
    dist = dot(field.normal, dir);
    dir = field.normal;
    break;
  }
  return dir * field.strength * (1 - clamp(dist * field.range_inverse, 0.0, 1.0));
}

void simulate_particle(inout Particle particle){
  for(uint i=0; i<field_count; ++i){
    ParticleForceField field = s_force_fields[i];
    vec3 force = evaluate_force_field(field, particle);
    particle.velocity += force * dt;
  }
}

void simulate_particle@after(inout Particle particle){
  particle.position += particle.velocity * dt;
  particle.life -= dt;
}

void main(){
  uint alive = alive_count;
  uint field_count = min(particle_force_field_count, FORCEFIELDS);
  if(gl_WorkGroupID < field_count){
    uint id = gl_WorkGroupID;
    s_force_fields[id] = particle_force_fields[id];
  }
  groupMemoryBarrier();

  if(gl_LocalInvocationID.x < alive){
    uint id = alive_particles.indices[gl_LocalInvocationID.x];
    Particle particle = particles[id];
    if(0 < particle.life){
      simulate_particle(particle);
      particles[id] = particle;

      uint new_index = AtomicAdd(draw_args.x, 6) / 6;
      alive_particles_1.indices[new_index] = id;
    }else{
      uint dead = AtomicAdd(dead_count, 1);
      dead_particles.indices[dead] = id;
    }
  }
}
