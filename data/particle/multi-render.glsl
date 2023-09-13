#section VERTEX_SHADER
#include (trial:trial "random.glsl")
out flat uint sprite;

void derive_particle_properties@after(in Particle particle, in uint vertex_id, in mat4 model_matrix){
  sprite = (particle.color >> 24) & uint(0x00000007);
  if(sprite == 0x7){
    // We use the particle instance ID as a random seed to keep the
    // selection consistent between frames.
    uint instance = gl_VertexID / 6;
    float id = float(alive_particles_0[instance]);
    sprite = uint(random(id) * float(0x7));
  }
}

#section FRAGMENT_SHADER
uniform sampler2DArray particle_tex;
in vec3 world_position;
in vec2 uv;
in float size;
in vec4 particle_color;
in flat uint sprite;
out vec4 color;

void main(){
  color = texture(particle_tex, vec3(uv, float(sprite)))*particle_color;
  color.xyz = pow(color.xyz, vec3(gamma));
}
