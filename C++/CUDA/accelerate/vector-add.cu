#include <stdio.h>
#include <assert.h>

inline cudaError_t checkCuda(cudaError_t result)
{
	if (result != cudaSuccess) {
		fprintf(stderr, "CUDA Runtime Error: %s\n", cudaGetErrorString(result));
		assert(result == cudaSuccess);
	}
	return result;
}

void initWith(float num, float *a, int N)
{
	for (int i = 0; i < N; ++i)
		a[i] = num;
}

__global__
void addVectorsInto(float *result, float *a, float *b, int N)
{
	int index = threadIdx.x + blockIdx.x*blockDim.x;
	int stride = gridDim.x * blockDim.x;

	for (int i = index; i < N; i += stride)
		result[i] = a[i] + b[i];
}

void checkElementsAre(float target, float *array, int N)
{
	for (int i = 0; i < N; i++) {
		if (array[i] != target) {
			printf("FAIL: array[%d] - %0.0f does not equal %0.0f\n", i, array[i], target);
			exit(1);
		}
	}
	printf("SUCCESS! All values added correctly.\n");
}

int main(void)
{
	const int N = 2 << 20;
	size_t size = N * sizeof(float);

	float *a;
	float *b;
	float *c;

	checkCuda( cudaMallocManaged(&a, size) );
	checkCuda( cudaMallocManaged(&b, size) );
	checkCuda( cudaMallocManaged(&c, size) );

	initWith(3, a, N);
	initWith(4, b, N);
	initWith(0, c, N);

	const size_t threads_per_block = 32;
	const size_t block_size = (N + threads_per_block - 1) / threads_per_block; // at most one extra block

	addVectorsInto<<<block_size,threads_per_block>>>(c, a, b, N);
	checkCuda( cudaGetLastError() );

	checkCuda( cudaDeviceSynchronize() );
	checkElementsAre(7, c, N);

	cudaFree(a);
	cudaFree(b);
	cudaFree(c);
}
