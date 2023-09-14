#section VERTEX_SHADER
uniform texture1D particle_data;
layout(location = 0) in vec3 pos;
layout(location = 1) in vec3 vel;
layout(location = 2) in float life;

struct Particle{
  vec3 position;
  vec3 velocity;
  float life;
  float max_life;
  float rotational_velocity;
  float size_begin;
  float size_end;
  int color;
};

#include (trial:trial "particle/render-common.glsl")

void main(){
  uint vertex_id = gl_VertexID;
  uint instance = gl_InstanceID;

  vec4 a = texelFetch(particle_data, instance*6+0, 0);
  
  Particle particle = Particle(pos,
                               vel,
                               life,
                               a.x,
                               a.y,
                               a.z,
                               a.w,
                               floatBitsToUInt(texelFetch(particle_data, instance*6+1, 0).r));
  mat4 model_matrix = mat4(texelFetch(particle_data, instance*6 + 2, 0),
                           texelFetch(particle_data, instance*6 + 3, 0),
                           texelFetch(particle_data, instance*6 + 4, 0),
                           texelFetch(particle_data, instance*6 + 5, 0));

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
