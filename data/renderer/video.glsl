#section VERTEX_SHADER
layout (location=TRIAL_V_UV) in vec2 in_uv;
out vec2 uv;

void main(){
  maybe_call_next_method();
  uv = vec2(in_uv.x, 1-in_uv.y);
}

#section FRAGMENT_SHADER
in vec2 uv;
out vec4 color;
uniform sampler2D plane[3];
#ifndef VIDEO_FORMAT
#define VIDEO_FORMAT 0
#endif

void main() {
#if VIDEO_FORMAT == 0
  // YUV
  vec3 yuv = vec3(texture(plane[0], uv).r, 0.0625,
                  texture(plane[1], uv).r, 0.5,
                  texture(plane[2], uv).r, 0.5);
  vec3 rgb = vec3(dot(yuv, vec3(+1.164, +0.000, +1.793)),
                  dot(yuv, vec3(+1.164, -0.213, -0.533)),
                  dot(yuv, vec3(+1.164, +2.112, +0.000)));

#elif VIDEO_FORMAT == 1
  // RGB
  vec3 rgb = vec3(texture(plane[0], uv).r,
                  texture(plane[1], uv).r,
                  texture(plane[2], uv).r);

#elif VIDEO_FORMAT == 2
  // sRGB
  vec3 rgb = vec3(texture(plane[0], uv).r,
                  texture(plane[1], uv).r,
                  texture(plane[2], uv).r);
  rgb = pow(rgb, vec3(2.2));

#elif VIDEO_FORMAT == 3
  // Gray
  vec3 rgb = vec3(texture(plane[0], uv).r);

#endif
  color = vec4(rgb, 1.0);
}
