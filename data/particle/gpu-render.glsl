#section VERTEX_SHADER
#extension GL_ARB_shader_storage_buffer_object : require
uniform mat4 model_matrix;

#include (trial:trial "particle/render-common.glsl")

void main(){
  int vertex_id = gl_VertexID % 6;
  int instance = gl_VertexID / 6;
  Particle particle = particles[alive_particles_0[instance]];
  derive_particle_properties(particle, vertex_id, model_matrix);
}

#section FRAGMENT_SHADER
#extension GL_ARB_shader_storage_buffer_object : require
#extension GL_KHR_blend_equation_advanced : enable
#ifdef GL_KHR_blend_equation_advanced
  // NOTE: Even though we don't use any of the gl_Sample* variables
  //       The AMD driver on Windows complains about it being used
  //       without this extension, so... we enable it explicitly.
  //       Gotta love AMD!!!!!!!!
  #extension GL_ARB_sample_shading : enable
  layout(blend_support_all_equations) out;
#endif

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
