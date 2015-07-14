#version 330 core

uniform mat4 M;
uniform mat4 VP;

attribute vec3 pos;
attribute vec2 texcoord;

varying vec2 frag_texcoord;

void main() 
{ 
	gl_Position = VP * M * vec4(pos, 1.0f); 
	frag_texcoord = texcoord;
}