#section VERTEX_SHADER
uniform sampler1D particle_data;
layout(location = 0) in vec3 pos;
layout(location = 1) in vec3 vel;
layout(location = 2) in float life;
layout(location = 3) in float in_prop;

struct Particle{
  vec3 position;
  vec3 velocity;
  float life;
  float max_life;
  float rotational_velocity;
  float size_begin;
  float size_end;
  uint color;
};

#include (trial:trial "particle/render-common.glsl")

void main(){
  int vertex_id = gl_VertexID % 6;
  int prop = floatBitsToInt(in_prop) / 4;
  vec4 a = texelFetch(particle_data, prop+0, 0);
  uint c = floatBitsToUint(texelFetch(particle_data, prop+1, 0).r);

  Particle particle = Particle(pos, vel, life, a.x, a.y, a.z, a.w, c);
  mat4 model_matrix = mat4(texelFetch(particle_data, prop+2, 0),
                           texelFetch(particle_data, prop+3, 0),
                           texelFetch(particle_data, prop+4, 0),
                           texelFetch(particle_data, prop+5, 0));

  derive_particle_properties(particle, vertex_id, model_matrix);
}

#section FRAGMENT_SHADER
uniform sampler2D particle_tex;
in vec3 world_position;
in vec2 uv;
in float size;
in vec4 particle_color;
out vec4 color;

void main(){
  color = texture(particle_tex, uv)*particle_color;
  color.xyz = pow(color.xyz, vec3(gamma));
}
