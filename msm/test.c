#include "msm.h"

#include <stdio.h>

int main(int argc, char* argv[]) {
    if (argc < 6) {
        fputs("test <path> <northweast latitude> <northwest longitude> <southeast latitude> <southeast longitude> <time>\n", stderr);
        return 1;
    }

    const char* path = argv[1];
    float nwLatitude;
    sscanf(argv[2], "%f", &nwLatitude);
    float nwLongitude;
    sscanf(argv[3], "%f", &nwLongitude);
    float seLatitude;
    sscanf(argv[4], "%f", &seLatitude);
    float seLongitude;
    sscanf(argv[5], "%f", &seLongitude);
    int time;
    sscanf(argv[6], "%d", &time);

    size_t count = surface(path, nwLatitude, nwLongitude, seLatitude, seLongitude, time, 0, 0);
    Surface* surfaces = malloc(sizeof(Surface)*count);
    count = surface(path, nwLatitude, nwLongitude, seLatitude, seLongitude, time, surfaces, count);
    for (size_t n = 0; n < count; ++n) {
        printf("%.4f, %.4f, %.1f, %.1f, %.2f, %.2f, %.1f, %d, %.1f, %d, %d, %d, %d\n",
               surfaces[n].latitude,
               surfaces[n].longitude,
               surfaces[n].airPressure,
               surfaces[n].surfaceAirPressure,
               surfaces[n].eastwardWind,
               surfaces[n].northwardWind,
               surfaces[n].airTemperature,
               surfaces[n].relativeHumidity,
               surfaces[n].rainfallRate,
               surfaces[n].upperCloudiness,
               surfaces[n].midCloudiness,
               surfaces[n].lowCloudiness,
               surfaces[n].cloudAmount);
    }
    free(surfaces);

    return 0;
}
