#section COMPUTE_SHADER
#extension GL_ARB_compute_shader : require
#extension GL_ARB_shader_storage_buffer_object : require

layout (local_size_x = SIMULATE_THREADS, local_size_y = 1, local_size_z = 1) in;
#define FORCEFIELDS 32
uint field_count = 0u;
shared ParticleForceField s_force_fields[FORCEFIELDS];

vec3 evaluate_force_field_point(in ParticleForceField field, in Particle particle){
  vec3 dir = field.position - particle.position;
  return dir * field.strength * (1 - clamp(length(dir) * field.inv_range, 0.0, 1.0));
}

vec3 evaluate_force_field_direction(in ParticleForceField field, in Particle particle){
  return field.normal * field.strength;
}

vec3 evaluate_force_field_plane(in ParticleForceField field, in Particle particle){
  float dist = dot(field.normal, particle.position - field.position);
  return field.normal * field.strength * (1 - clamp(dist * field.inv_range, 0.0, 1.0));
}

vec3 evaluate_force_field_vortex(in ParticleForceField field, in Particle particle){
  vec3 dir = particle.position - field.position;
  float t0 = dot(field.normal, dir) / dot(field.normal, field.normal);
  float dist = distance(particle.position, field.position*t0);
  vec3 perp = normalize(cross(field.normal, dir));
  return perp * field.strength * (1 - clamp(dist * field.inv_range, 0.0, 1.0));
}

vec3 evaluate_force_field_sphere(in ParticleForceField field, in Particle particle){
  vec3 dir = field.position - particle.position;
  float dist = length(dir);
  if(dist < field.range){
    vec3 push = normalize(-dir);
    vec3 slide = cross(cross(particle.velocity, push), -push);
    return (slide-particle.velocity) / dt;
  }
  return vec3(0);
}

vec3 evaluate_force_field_planet(in ParticleForceField field, in Particle particle){
  vec3 dir = field.position - particle.position;
  float dist = length(dir);
  if(dist < field.range){
    // Same as sphere above.
    vec3 push = normalize(-dir);
    vec3 slide = cross(cross(particle.velocity, push), -push);
    return (slide-particle.velocity) / dt;
  }else{
    return dir * field.strength / (dist * dist);
  }
}

vec3 evaluate_force_field_brake(in ParticleForceField field, in Particle particle){
  return particle.velocity * -field.strength;
}

vec3 evaluate_force_field(in ParticleForceField field, in Particle particle){
  switch(field.type){
  case 1: return evaluate_force_field_point(field, particle);
  case 2: return evaluate_force_field_direction(field, particle);
  case 3: return evaluate_force_field_plane(field, particle);
  case 4: return evaluate_force_field_vortex(field, particle);
  case 5: return evaluate_force_field_sphere(field, particle);
  case 6: return evaluate_force_field_planet(field, particle);
  case 7: return evaluate_force_field_brake(field, particle);
  default: return vec3(0);
  }
}

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
