#include "msm.h"

#include <stdio.h>

int main(int argc, char* argv[]) {
    if (argc < 6) {
        fputs("surface <path> <northweast latitude> <northwest longitude> <southeast latitude> <southeast longitude> <time>\n", stderr);
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

    size_t count = get_surface_items(path, nwLatitude, nwLongitude, seLatitude, seLongitude, time, 0, 0);
    surface_item* items = malloc(sizeof(surface_item)*count);
    count = get_surface_items(path, nwLatitude, nwLongitude, seLatitude, seLongitude, time, items, count);
    for (size_t n = 0; n < count; ++n) {
        printf("%.4f, %.4f, %.1f, %.1f, %.2f, %.2f, %.1f, %d, %.1f, %d, %d, %d, %d\n",
               items[n].latitude,
               items[n].longitude,
               items[n].air_pressure,
               items[n].surface_air_pressure,
               items[n].eastward_wind,
               items[n].northward_wind,
               items[n].air_temperature,
               items[n].relative_humidity,
               items[n].rainfall_rate,
               items[n].upper_cloudiness,
               items[n].mid_cloudiness,
               items[n].low_cloudiness,
               items[n].cloud_amount);
    }
    free(items);

    return 0;
}
