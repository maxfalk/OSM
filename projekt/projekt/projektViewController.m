//
//  projektViewController.m
//  projekt
//
//  Created by Babak Toghiani-Rizi on 15/04/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#define SYSTEM_VERSION_GREATER_THAN_OR_EQUAL_TO(v)  ([[[UIDevice currentDevice] systemVersion] compare:v options:NSNumericSearch] != NSOrderedAscending)

#import "projektViewController.h"


@interface projektViewController ()
@property (weak, nonatomic) IBOutlet UITextField *usernameField;
@property (weak, nonatomic) IBOutlet UITextField *passwordField;
@property (weak, nonatomic) IBOutlet UIActivityIndicatorView *wheel;

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
    
   // _passwordField = [[UITextField alloc] init];
    _passwordField.borderStyle = UITextBorderStyleRoundedRect;
    _passwordField.textColor = [UIColor blackColor];
   // _passwordField.font = [UIFont systemFontOfSize:12.0];
    [_passwordField setSecureTextEntry:YES];
    
    _usernameField.delegate = self;
    _passwordField.delegate = self;
    
    _wheel.hidden=YES;
    
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
    NSLog(@"lat:%f long:%f", manager.location.coordinate.latitude, manager.location.coordinate.latitude);
}


- (BOOL)textFieldShouldReturn:(UITextField *)theTextField {
    if (theTextField == _passwordField) {
        [theTextField resignFirstResponder];
    } else if (theTextField == _usernameField) {
        [_passwordField becomeFirstResponder];
    }
    return YES;
}

-(BOOL)textViewShouldEndEditing:(UITextView *)textView{
    [textView resignFirstResponder];
    return YES;
}

- (IBAction)loginButtonPressed:(id)sender {
    _wheel.hidden = NO;
    [_wheel startAnimating];

    [_loginButton setTitle:@"" forState:UIControlStateNormal];
    [_passwordField resignFirstResponder];
    
    double delayInSeconds = 2.0;
    dispatch_time_t popTime = dispatch_time(DISPATCH_TIME_NOW, (int64_t)(delayInSeconds * NSEC_PER_SEC));
    dispatch_after(popTime, dispatch_get_main_queue(), ^(void){
         [self performSegueWithIdentifier:@"login" sender:self];
        _wheel.hidden=YES;
        [_wheel stopAnimating];
        [_loginButton setTitle:@"Logga in" forState:UIControlStateNormal];
    });
    
    
    
}




@end


