in vec2 tex_coord;
out vec4 color;
uniform sampler2D previous_pass;
uniform float threshold = 2.0;

void main(){
  color = texture(previous_pass, tex_coord);
  float sum = color.x+color.y+color.z;
  if(sum <= threshold){
    color = vec4(0);
  }
}
