const vec3 BILLBOARD[6] = vec3[6](
  vec3(-1, -1, 0),
  vec3(+1, -1, 0),
  vec3(-1, +1, 0),
  vec3(-1, +1, 0),
  vec3(+1, -1, 0),
  vec3(+1, +1, 0)
);

uniform float motion_blur = 0.0;
uniform vec3 offset = vec3(0.0);
out vec3 world_position;
out vec3 view_position;
out vec2 uv;
out float size;
out vec4 particle_color;

void derive_particle_properties(in Particle particle, in int vertex_id, in mat4 model_matrix){
  float interpolation = 1.0f - particle.life / particle.max_life;
  size = mix(particle.size_begin, particle.size_end, interpolation);
  float opacity = clamp(mix(1.0f, 0.0f, interpolation), 0.0f, 1.0f);
  // The explicit conversion to float should not be needed but
  // apparently some versions of the NVidia drivers for Linux choke
  // without it.
  particle_color.r = ((float) ((particle.color >> 0)  & uint(0x000000FF))) / 255.0f;
  particle_color.g = ((float) ((particle.color >> 8)  & uint(0x000000FF))) / 255.0f;
  particle_color.b = ((float) ((particle.color >> 16) & uint(0x000000FF))) / 255.0f;
  particle_color.a = opacity;

  vec3 vertex = BILLBOARD[vertex_id];
  uv = vertex.xy * 0.5 + 0.5;
  // High bits mark the mirroring
  uv.x = (uint(0) < (particle.color & uint(0x80000000))) ? 1.0f - uv.x : uv.x;
  uv.y = (uint(0) < (particle.color & uint(0x40000000))) ? 1.0f - uv.y : uv.y;

  // Rotate it
  float rotation = interpolation * particle.rotational_velocity;
  mat2 rot = mat2(+cos(rotation), -sin(rotation),
                  +sin(rotation), +cos(rotation));
  vertex.xy = rot * vertex.xy;
  // Scale it
  vertex *= size;
  // Scale along the motion vector in view space
  vec3 velocity = mat3(view_matrix) * particle.velocity;
  vertex += offset;
  vertex += dot(vertex, velocity) * velocity * motion_blur;

  // Check if billboard or not
  if(uint(0) == (particle.color & uint(0x20000000))){
    world_position = (model_matrix * vec4(particle.position, 1)).xyz;
    view_position = (view_matrix * vec4(world_position, 1)).xyz;
    gl_Position = projection_matrix * vec4(view_position+vertex, 1);
  }else{
    vec4 position = model_matrix * vec4(particle.position+vertex, 1);
    world_position = position.xyz;
    position = view_matrix * position;
    view_position = position.xyz;
    gl_Position = projection_matrix * position;
  }
}
