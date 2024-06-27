// We need to include it as a uniform, but the struct include
// would not create the correct uniform block.
// #include trial::morph-data
#define SIMULTANEOUS_MORPHS 8
layout(std140) uniform MorphData {
  int count;
  float weights[SIMULTANEOUS_MORPHS];
  int indices[SIMULTANEOUS_MORPHS];
} morph_data;

uniform sampler1DArray morph_targets;

void morph_vertex(inout vec3 position, inout vec3 normal, inout vec2 uv){
  int vid = gl_VertexID*3;
  for(int i=0; i<min(morph_data.count, SIMULTANEOUS_MORPHS); ++i){
    int index = morph_data.indices[i];
    float weight = morph_data.weights[i];
    position += weight * texelFetch(morph_targets, ivec2(index, vid+0), 0).xyz;
    normal += weight * texelFetch(morph_targets, ivec2(index, vid+1), 0).xyz;
    uv += weight * texelFetch(morph_targets, ivec2(index, vid+2), 0).xy;
  }
}
