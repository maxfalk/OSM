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
    
    UIColor *purple = [UIColor colorWithRed:0.36 green:0.15 blue:0.48 alpha:1];
    UIColor *green = [UIColor colorWithRed:0.07 green:0.52 blue:0.49 alpha:1];
    UIColor *blue = [UIColor colorWithRed:0.06 green:0.44 blue:0.74 alpha:1];
    UIColor *babyBlue = [UIColor colorWithRed:0.4 green:0.6 blue:0.72 alpha:1];
    
    self.navigationController.navigationBar.barTintColor = babyBlue;

}

- (void)viewWillAppear:(BOOL)animated
{
    [self.navigationController setNavigationBarHidden:YES animated:animated];
    [super viewWillAppear:animated];
}

- (void)viewWillDisappear:(BOOL)animated {
    [self.navigationController setNavigationBarHidden:NO animated:animated];
    [super viewWillDisappear:animated];
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


