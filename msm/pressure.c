#include "msm.h"

#include <stdio.h>

int main(int argc, char* argv[]) {
    if (argc < 6) {
        fputs("pressure <path> <northweast latitude> <northwest longitude> <southeast latitude> <southeast longitude> <time>\n", stderr);
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

    size_t count = pressure(path, nwLatitude, nwLongitude, seLatitude, seLongitude, time, 0, 0);
    Pressure *pressures = malloc(sizeof(Pressure)*count);
    count = pressure(path, nwLatitude, nwLongitude, seLatitude, seLongitude, time, pressures, count);
    for (size_t n = 0; n < count; ++n) {
        printf("%.4f, %.4f, %d, %.1f, %.2f, %.2f, %.2f, %.1f, %d\n",
               pressures[n].latitude,
               pressures[n].longitude,
               pressures[n].airPressure,
               pressures[n].geopotentialHeight,
               pressures[n].lagrangianTendencyOfAirPressure,
               pressures[n].eastwardWind,
               pressures[n].northwardWind,
               pressures[n].airTemperature,
               pressures[n].relativeHumidity);
    }
    free(pressures);

    return 0;
}
