#section COMPUTE_SHADER

layout (local_size_x = EMIT_THREADS, local_size_y = 1, local_size_z = 1) in;
uniform sampler2D random_tex;

vec3 read_vertex(uint i){
  return vec3(mesh_vertex_buffer[i*mesh_vertex_position_stride + 0],
              mesh_vertex_buffer[i*mesh_vertex_position_stride + 1],
              mesh_vertex_buffer[i*mesh_vertex_position_stride + 2]);
}

vec3 read_normal(uint i){
  return vec3(mesh_vertex_buffer[i*mesh_vertex_normal_stride + 3],
              mesh_vertex_buffer[i*mesh_vertex_normal_stride + 4],
              mesh_vertex_buffer[i*mesh_vertex_normal_stride + 5]);
}

void main(){
  uint emit = real_emit_count;
  vec3 randoms = texture(random_tex, vec2(float(gl_LocalInvocationID.x)/float(EMIT_THREADS), randomness+tt)).xyz;
  uint tri = uint((mesh_index_count -1)/3*randoms.x) * 3;
  uint i0 = mesh_index_buffer[tri+0];
  uint i1 = mesh_index_buffer[tri+1];
  uint i2 = mesh_index_buffer[tri+2];
  vec3 p0 = read_vertex(i0);
  vec3 p1 = read_vertex(i1);
  vec3 p2 = read_vertex(i2);
  vec3 n0 = read_normal(i0);
  vec3 n1 = read_normal(i1);
  vec3 n2 = read_normal(i2);

  float f = randoms.x;
  float g = randoms.y;
  if(f+g > 1){
    f = 1-f;
    g = 1-g;
  }

  vec3 pos = p0 + f*(p1-p0) + g*(p2-p0);
  vec3 nor = n0 + f*(n1-n0) + g*(n2-n0);
  pos = (model_matrix * vec4(pos, 1)).xyz;
  nor = normalize(mat3(model_matrix)*nor);
  float starting_size = particle_size + particle_size * particle_random_factor * (randoms.y-0.5);

  Particle particle;
  particle.position = pos;
  particle.velocity = particle_normal_factor * (nor + particle_random_factor*(randoms-0.5));
  particle.rotational_velocity = particle_rotation * particle_random_factor * (randoms.z-0.5);
  particle.max_life = particle_lifespan + particle_lifespan * particle_lifespan_randomness * (randoms.x-0.5);
  particle.life = particle.max_life;
  particle.size_begin = particle_starting_size;
  particle.size_end = particle_starting_size * particle_scaling;

  uint dead = atomicAdd(dead_count, -1)-1;
  uint new_index = dead_particles.indices[dead];
  particles[new_index] = particle;

  uint alive = atomicAdd(alive_count, +1);
  alive_particles_0[alive] = new_index;
}
