#include <algorithm>
#include <array>
#include <cmath>
#include <functional>
#include "msm.h"
#include "netcdf.hpp"

using namespace netcdf;
using namespace std;

static const float MIN_LATITUDE = 22.4;
static const float MAX_LATITUDE = 47.6;
static const float SURFACE_STEP_LATITUDE = 0.05;
static const float PRESSURE_STEP_LATITUDE = 0.1;
static const float MIN_LONGITUDE = 120.0;
static const float MAX_LONGITUDE = 150.0;
static const float SURFACE_STEP_LONGITUDE = 0.0625;
static const float PRESSURE_STEP_LONGITUDE = 0.125;

template<typename T>
static void readDouble(const File& file,
                       const string& name,
                       const array<size_t, 3>& start,
                       const array<size_t, 3>& count,
                       Surface* surfaces,
                       T Surface::*property,
                       const function<T (double)>& fn);

size_t surface(const char* path,
               const float nwLatitude,
               const float nwLongitude,
               const float seLatitude,
               const float seLongitude,
               const int time,
               Surface *surfaces,
               const size_t surfaceCount) {
    const size_t nwLatitudeIndex = floorf((MAX_LATITUDE - min(max(nwLatitude, MIN_LATITUDE), MAX_LATITUDE))/SURFACE_STEP_LATITUDE);
    const size_t nwLongitudeIndex = floorf((min(max(nwLongitude, MIN_LONGITUDE), MAX_LONGITUDE) - MIN_LONGITUDE)/SURFACE_STEP_LONGITUDE);
    const size_t seLatitudeIndex = ceilf((MAX_LATITUDE - min(max(seLatitude, MIN_LATITUDE), MAX_LATITUDE))/SURFACE_STEP_LATITUDE);
    const size_t seLongitudeIndex = ceilf((min(max(seLongitude, MIN_LONGITUDE), MAX_LONGITUDE) - MIN_LONGITUDE)/SURFACE_STEP_LONGITUDE);
    if (nwLatitudeIndex >= seLatitudeIndex || nwLongitudeIndex >= seLongitudeIndex) {
        return 0;
    }

    const size_t latitudeCount = seLatitudeIndex - nwLatitudeIndex + 1;
    const size_t longitudeCount = seLongitudeIndex - nwLongitudeIndex + 1;
    if (!surfaces) {
        return latitudeCount*longitudeCount;
    }
    else if (surfaceCount < latitudeCount*longitudeCount) {
        return -1;
    }

    for (size_t lat = 0; lat < latitudeCount; ++lat) {
        const float latitude = MAX_LATITUDE - (nwLatitudeIndex + lat)*SURFACE_STEP_LATITUDE;
        for (size_t lon = 0; lon < longitudeCount; ++lon) {
            const float longitude = MIN_LONGITUDE + (nwLongitudeIndex + lon)*SURFACE_STEP_LONGITUDE;
            surfaces[lat*longitudeCount + lon].latitude = latitude;
            surfaces[lat*longitudeCount + lon].longitude = longitude;
        }
    }

    const array<size_t, 3> start = { size_t(time), nwLatitudeIndex, nwLongitudeIndex };
    const array<size_t, 3> count = { 1, latitudeCount, longitudeCount };
    try {
        File file(path);

        readDouble<float>(file, "psea", start, count, surfaces, &Surface::airPressure, [](double v) { return round(v/10)/10.0f; });
        readDouble<float>(file, "sp", start, count, surfaces, &Surface::surfaceAirPressure, [](double v) { return round(v/10)/10.0; });
        readDouble<float>(file, "u", start, count, surfaces, &Surface::eastwardWind, [](double v) { return round(v*100.0)/100.0; });
        readDouble<float>(file, "v", start, count, surfaces, &Surface::northwardWind, [](double v) { return round(v*100.0)/100.0; });
        readDouble<float>(file, "temp", start, count, surfaces, &Surface::airTemperature, [](double v) { return round((v - 273.15)*10)/10.0; });
        readDouble<int>(file, "rh", start, count, surfaces, &Surface::relativeHumidity, [](double v) { return round(v); });
        readDouble<float>(file, "r1h", start, count, surfaces, &Surface::rainfallRate, [](double v) { return round(v); });
        readDouble<int>(file, "ncld_upper", start, count, surfaces, &Surface::upperCloudiness, [](double v) { return round(v); });
        readDouble<int>(file, "ncld_mid", start, count, surfaces, &Surface::midCloudiness, [](double v) { return round(v); });
        readDouble<int>(file, "ncld_low", start, count, surfaces, &Surface::lowCloudiness, [](double v) { return round(v); });
        readDouble<int>(file, "ncld", start, count, surfaces, &Surface::cloudAmount, [](double v) { return round(v); });
    }
    catch (Exception e) {
        return -1;
    }

    return latitudeCount*longitudeCount;
}

template<typename T>
static void readDouble(const File& file,
                       const string& name,
                       const array<size_t, 3>& start,
                       const array<size_t, 3>& count,
                       Surface* surfaces,
                       T Surface::*property,
                       const function<T (double)>& converter) {
    short v[count[1]*count[2]];
    const Var var(file.var(name));
    const double scaleFactor = var.attDouble("scale_factor");
    const double offset = var.attDouble("add_offset");
    var.get(start, count, v);
    for (size_t n = 0; n < count[1]*count[2]; ++n) {
        surfaces[n].*property = converter(v[n]*scaleFactor + offset);
    }
}
