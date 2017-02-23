#version 330 core

in vec2 tex_coord;
out vec4 color;

uniform sampler2D tex_data;

void main(){
  //color = vec4(1.0f, 1.0f, 0.0f, 1.0f);
  color = texture(tex_data, tex_coord, 1.0);
}
