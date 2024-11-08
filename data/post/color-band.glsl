#section FRAGMENT_SHADER
in vec2 uv;
out vec4 color;
uniform sampler2D previous_pass;
uniform float bands = 8.0;
uniform float pixel_size = 4.0;

float luminance(vec3 color) {
  return dot(color, vec3(0.299f, 0.587f, 0.114f));
}

float bayer(vec2 pixelpos){
  ivec2 ppos = ivec2(pixelpos);
  int sum = 0;
  for(int i = 0; i<DITHER_LEVEL; i++){
    ivec2 t = ppos & 1;
    sum = sum * 4 | (t.x ^ t.y) * 2 | t.x;
    ppos /= 2;
  }
  return float(sum) / float(1 << (2 * DITHER_LEVEL));
}

void main(){
  ivec2 size = textureSize(previous_pass, 0).xy;
  ivec2 puv = ivec2(uv*size/pixel_size);
  color = texelFetch(previous_pass, ivec2(puv*pixel_size), 0);
  vec3 c1 = floor(color.rgb*bands)/bands;
  vec3 c2 = c1+1/bands;
  float dither = bayer(puv) - 0.5;
  color.rgb = vec3(mix(c1, c2, float(luminance(c1)+dither < 0.5)));
}
