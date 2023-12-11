#section FRAGMENT_SHADER
in vec2 uv;
out vec4 color;
uniform sampler2D previous_pass;
uniform float intensity = 1.0;

void main(){
  vec2 blur_size = intensity / vec2(textureSize(previous_pass, 0));
  vec4 sum = vec4(0.0);
  for (int x=-4; x<=4; x++){
    for (int y=-4; y<=4; y++){
      sum += texture(previous_pass, uv + vec2(x,y)*blur_size);
    }
  }
  color = sum * (1/81.0);
}
