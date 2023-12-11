#section FRAGMENT_SHADER
in vec2 uv;
out vec4 color;
uniform sampler2D previous_pass;
uniform sampler2D bloom_cutoff;
uniform float intensity = 1.0;
uniform float weight[25] = float[](
    0.0029150245, 0.013064233, 0.021539278, 0.013064233, 0.0029150245,
    0.013064233, 0.058549833, 0.09653235, 0.058549833, 0.013064233,
    0.021539278, 0.09653235, 0.15915494, 0.09653235, 0.021539278,
    0.013064233, 0.058549833, 0.09653235, 0.058549833, 0.013064233,
    0.0029150245, 0.013064233, 0.021539278, 0.013064233, 0.0029150245);

void main(){
  vec2 blur_size = intensity / vec2(textureSize(bloom_cutoff, 0));
  vec3 sum = vec3(0.0);
  for (int x=-2; x<=2; x++){
    for (int y=-2; y<=2; y++){
      sum += texture(bloom_cutoff, uv + vec2(x,y)*blur_size).xyz * weight[(y+2)*5+x+2];
    }
  }
  color = texture(previous_pass, uv) + vec4(sum, 1);
}
