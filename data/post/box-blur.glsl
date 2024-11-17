#section FRAGMENT_SHADER
uniform float intensity = 1.0;
uniform int kernel_size = 9;

vec4 post_process(sampler2D previous_pass, vec2 uv){
  vec2 blur_size = intensity / vec2(textureSize(previous_pass, 0));
  vec4 sum = vec4(0.0);
  int kern = kernel_size / 2;
  for (int x=-kern; x<=kern; x++){
    for (int y=-kern; y<=kern; y++){
      sum += texture(previous_pass, uv + vec2(x,y)*blur_size);
    }
  }
  return sum * (1.0/(kernel_size*kernel_size));
}
