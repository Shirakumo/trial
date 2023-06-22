#section FRAGMENT_SHADER
#include "pbr-common.glsl"
const uint sample_count = 1024u;
out vec4 color;

// Override
float G_sggx(float NdotV, float r){
  float k = (r*r) / 2.0;
  return NdotV / (NdotV * (1.0 - k) + k);
}

void main(){
  float NdotV = uv.x;
  float roughness = uv.y;

  vec3 V;
  V.x = sqrt(1.0 - NdotV*NdotV);
  V.y = 0.0;
  V.z = NdotV;

  float A = 0.0;
  float B = 0.0; 

  vec3 N = vec3(0.0, 0.0, 1.0);
    
  for(uint i = 0u; i < sample_count; ++i){
    // generates a sample vector that's biased towards the
    // preferred alignment direction (importance sampling).
    vec2 Xi = hammersley(i, sample_count);
    vec3 H = importance_sample_ggx(Xi, N, roughness);
    vec3 L = normalize(2.0 * dot(V, H) * H - V);

    float NdotL = max(L.z, 0.0);
    float NdotH = max(H.z, 0.0);
    float VdotH = max(dot(V, H), 0.0);

    if(NdotL > 0.0){
      float G = G_smith(N, V, L, roughness);
      float G_Vis = (G * VdotH) / (NdotH * NdotV);
      float Fc = pow(1.0 - VdotH, 5.0);

      A += (1.0 - Fc) * G_Vis;
      B += Fc * G_Vis;
    }
  }
  A /= float(sample_count);
  B /= float(sample_count);
  color = vec4(A, B, 0, 1);
}
