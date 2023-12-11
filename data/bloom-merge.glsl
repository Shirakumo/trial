#section FRAGMENT_SHADER
in vec2 uv;
out vec4 color;
uniform sampler2D previous_pass;
uniform sampler2D bloom_cutoff;
uniform float intensity = 1.0;

void main(){
  vec2 blur_size = intensity / vec2(textureSize(bloom_cutoff, 0));
  vec4 sum = vec4(0.0);
  for (int x=-3; x<=3; x++){
    for (int y=-3; y<=3; y++){
      sum += texture(bloom_cutoff, uv + vec2(x,y)*blur_size);
    }
  }
  color = texture(previous_pass, uv) + sum * (1/49.0);
}
