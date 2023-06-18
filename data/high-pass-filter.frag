in vec2 uv;
out vec4 color;
uniform sampler2D previous_pass;
uniform float threshold = 1.0;

void main(){
  color = texture(previous_pass, uv);
  float sum = color.x+color.y+color.z;
  if(sum >= threshold){
    color = vec4(0);
  }
}
