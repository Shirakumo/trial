#section FRAGMENT_SHADER
uniform sampler2D previous_pass;
in vec2 uv;
out float color;

float color_luminance(vec3 color){
  return dot(color, vec3(0.299, 0.587, 0.114));
}

void main(){
  color = color_luminance(texture(previous_pass, uv).xyz);
}
