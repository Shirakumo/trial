in vec2 texcoord;
out vec4 color;
uniform sampler2D previousPass;
uniform float radius = 1000;
uniform float angle = 0.8;

void main(){
  vec2 texSize = textureSize(previousPass, 0);
  vec2 center = texSize/2;
  vec2 tc = (texcoord * texSize) - center;
  float dist = length(tc);
  if (dist < radius) {
    float percent = (radius - dist) / radius;
    float theta = percent * percent * angle * 8.0;
    float s = sin(theta);
    float c = cos(theta);
    tc = vec2(dot(tc, vec2(c, -s)), dot(tc, vec2(s, c)));
  }
  color = texture2D(previousPass, (tc+center) / texSize);
}
