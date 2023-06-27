#section COMPUTE_SHADER
layout (local_size_x = SIMULATE_THREADS, local_size_y = 1, local_size_z = 1) in;
#define FORCEFIELDS 32
uint field_count = 0;
shared ParticleForceField s_force_fields[FORCEFIELDS];

vec3 evaluate_force_field(ParticleForceField field, Particle particle){
  vec3 dir = field.position - particle.position;
  float dist = 0.0;
  switch(field.type){
  case 0: // NONE
    return vec3(0);
  case 1: // POINT
    dist = length(dir);
    break;
  case 2: // GRAVITY
    dist = length(dir);
    dist *= dist;
    break;
  case 3: // PLANE
    dist = dot(field.normal, dir);
    dir = field.normal;
    break;
  case 4: // VORTEX
    dist = dot(field.normal, dir);
    dir = normalize(cross(field.normal, dir));
  }
  return dir * field.strength * (1 - clamp(dist * field.inv_range, 0.0, 1.0));
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
  field_count = min(particle_force_field_count, FORCEFIELDS);
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
      simulate_particle(particle);
      particles[id] = particle;

      uint new_index = atomicAdd(draw_args.x, 6) / 6;
      alive_particles_1[new_index] = id;
    }else{
      uint dead = atomicAdd(dead_count, +1);
      dead_particles[dead] = id;
    }
  }
}
