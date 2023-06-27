#section VERTEX_SHADER
uniform float motion_blur = 0.0;

const vec3 BILLBOARD[6] = vec3[6](
  vec3(-1, -1, 0),
  vec3(+1, -1, 0),
  vec3(-1, +1, 0),
  vec3(-1, +1, 0),
  vec3(+1, -1, 0),
  vec3(+1, +1, 0)
);

out vec3 world_position;
out vec3 view_position;
out vec2 uv;
out float size;
out vec4 particle_color;

void main(){
  uint vertex_id = gl_VertexID % 6;
  uint instance = gl_VertexID / 6;

  Particle particle = particles[alive_particles_0[instance]];
  float interpolation = 1.0f - particle.life / particle.max_life;
  size = mix(particle.size_begin, particle.size_end, interpolation);
  float opacity = clamp(mix(1.0f, 0.0f, interpolation), 0.0f, 1.0f);
  particle_color.r = ((particle.color >> 0)  & 0x000000FF) / 255.0f;
  particle_color.g = ((particle.color >> 8)  & 0x000000FF) / 255.0f;
  particle_color.b = ((particle.color >> 16) & 0x000000FF) / 255.0f;
  particle_color.a = opacity;
  
  vec3 vertex = BILLBOARD[vertex_id];
  uv = vertex.xy * 0.5 + 0.5;
  // High bits mark the mirroring
  uv.x = (0 < (particle.color & 0x10000000)) ? 1.0f - uv.x : uv.x;
  uv.y = (0 < (particle.color & 0x20000000)) ? 1.0f - uv.y : uv.y;
  
  // Rotate it
  float rotation = interpolation * particle.rotational_velocity;
  mat2 rot = mat2(+cos(rotation), -sin(rotation),
                  +sin(rotation), +cos(rotation));
  vertex.xy = rot * vertex.xy;
  // Scale it
  vertex *= size;
  // Scale along the motion vector in view space
  vec3 velocity = mat3(view_matrix) * particle.velocity;
  vertex += dot(vertex, velocity) * velocity * motion_blur;

  world_position = particle.position;
  view_position = (view_matrix * vec4(world_position, 1)).xyz;
  gl_Position = projection_matrix * vec4(view_position+vertex, 1);
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
}
