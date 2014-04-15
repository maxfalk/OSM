//
//  projektViewController.m
//  projekt
//
//  Created by Babak Toghiani-Rizi on 15/04/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import "projektViewController.h"

@interface projektViewController ()

@end

@implementation projektViewController

- (void)viewDidLoad
{
    [super viewDidLoad];
	// Do any additional setup after loading the view, typically from a nib.
    
    locationManager = [[CLLocationManager alloc] init];
    locationManager.delegate = self;
    [locationManager startUpdatingLocation];
    NSLog(@"latitude= %f longitude = %f",locationManager.location.coordinate.latitude, locationManager.location.coordinate.latitude);

}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (void)locationManager:(CLLocationManager *)manager didUpdateLocations:(NSArray *)locations{
    NSLog(@"latitude= %f longitude = %f", manager.location.coordinate.latitude, manager.location.coordinate.latitude);
}

@end


