#section COMPUTE_SHADER
#extension GL_ARB_compute_shader : require

layout (local_size_x = EMIT_THREADS, local_size_y = 1, local_size_z = 1) in;
uniform sampler2D random_tex;

vec3 read_vertex(uint i){
  return vec3(vertex_data[i*mesh_vertex_stride + 0],
              vertex_data[i*mesh_vertex_stride + 1],
              vertex_data[i*mesh_vertex_stride + 2]);
}

vec3 read_normal(uint i){
  return vec3(vertex_data[i*mesh_vertex_stride + 3],
              vertex_data[i*mesh_vertex_stride + 4],
              vertex_data[i*mesh_vertex_stride + 5]);
}

void main(){
  uint emit = real_emit_count;
  if(gl_GlobalInvocationID.x < emit){
    vec3 randoms = texture(random_tex, vec2(float(gl_GlobalInvocationID.x)/float(EMIT_THREADS), randomness)).xyz;

    // Evaluate the surface to emit on
    uint tri = uint((mesh_index_count/3)*randoms.z);
    uint i0 = index_data[tri*3+0];
    uint i1 = index_data[tri*3+1];
    uint i2 = index_data[tri*3+2];
    vec3 p0 = read_vertex(i0);
    vec3 p1 = read_vertex(i1);
    vec3 p2 = read_vertex(i2);
    vec3 n0 = read_normal(i0);
    vec3 n1 = read_normal(i1);
    vec3 n2 = read_normal(i2);

    // Use barycentric coordinates to randomly distribute on the triangle
    float f = randoms.x;
    float g = randoms.y;
    if(1 < f+g){
      f = 1-f;
      g = 1-g;
    }

    vec3 pos = p0 + f*(p1-p0) + g*(p2-p0);
    pos = (model_matrix * vec4(pos, 1)).xyz;
    vec3 nor = n0 + f*(n1-n0) + g*(n2-n0);
    nor = normalize(mat3(model_matrix)*nor);

    // Compute the particle properties
    Particle particle;
    particle.position = pos;
    particle.velocity = particle_velocity * (nor + particle_randomness*(randoms-0.5));
    particle.rotational_velocity = particle_rotation * particle_randomness * (randoms.z-0.5);
    particle.max_life = particle_lifespan + particle_lifespan * particle_lifespan_randomness * (randoms.x-0.5);
    particle.life = particle.max_life;
    particle.size_begin = particle_size + particle_size * particle_randomness * (randoms.y-0.5);
    particle.size_end = particle.size_begin * particle_scaling;

    // Bit masking bullshit
    uint bitmask = particle_color & 0xFF000000;
    particle.color = particle_color & 0x3FFFFFFF;
    particle.color |= ((randoms.x > 0.5f ? 1 : 0) << 31) & bitmask;
    particle.color |= ((randoms.y < 0.5f ? 1 : 0) << 30) & bitmask;

    // Update the lists
    uint dead = atomicAdd(dead_count, -1)-1;
    uint new_index = dead_particles[dead];
    particles[new_index] = particle;

    uint alive = atomicAdd(alive_count, +1);
    alive_particles_0[alive] = new_index;
  }
}
