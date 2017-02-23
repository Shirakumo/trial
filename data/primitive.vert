#version 330 core
  
layout (location = 0) in vec3 position;
layout (location = 1) in vec2 tex;

out vec2 tex_coord;

uniform mat4 transform;

void main(){
  gl_Position = transform * vec4(position, 1.0f);
  tex_coord = tex;
}
